//
//  FCTWebSocket.m
//  Trellis
//
//  Created by Babak Ghahremanpour on 11/28/12.
//
//

#import <SocketRocket/SRWebSocket.h>
#import <ReactiveCocoa/ReactiveCocoa.h>
#import "FCTWebSocket.h"

static const float FCTWS_INITIAL_RECONNECT_DELAY = 0.1;
static const NSUInteger FCTWS_MAX_RECONNECT_DELAY = 1200;

static void FCTWSLog(NSString *format, ...) {
#if DEBUG_WEB_SOCKETS
    va_list args;
    va_start(args, format);
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wformat-nonliteral"
    NSLogv([NSString stringWithFormat:@"FCTWSLog: %@", format], args);
#pragma clang diagnostic pop
    va_end(args);
#endif
}

static NSURL *stauntonServerURL(void) {
    return [NSURL URLWithString:@"ws://localhost:9160/"];
}

typedef enum {
    FCTWebSocketReadyStateOpened,
    FCTWebSocketReadyStateOpening,
    FCTWebSocketReadyStateClosed
} FCTWebSocketReadyState;

#pragma mark - Anonymous category

@interface FCTWebSocket () <SRWebSocketDelegate>

@property (nonatomic, strong) SRWebSocket *webSocket;

// For messages that haven't been sent (because the socket was closed).
@property (nonatomic, strong, readonly) NSMutableArray *sendQueue;

@property (nonatomic, assign) NSTimeInterval reconnectDelay;
@property (nonatomic, assign) FCTWebSocketReadyState wsReadyState;

@property (nonatomic, strong, readonly) NSMutableDictionary *callbacksByReqid;

// Typecast helper.
@property (nonatomic, strong, readonly) RACSubject *notificationSubject;

@end

@implementation FCTWebSocket

- (void)dealloc {
    [self.notificationSubject sendCompleted];
    
    for (NSString *key in [self.callbacksByReqid allKeys]) {
        RACSubject *subject = self.callbacksByReqid[key];
        [subject sendCompleted];
    }
    
    [self close];
}

- (instancetype)init {
    if (self = [super init]) {
        _callbacksByReqid = [NSMutableDictionary dictionary];
        _notificationSignal = [RACSubject subject];
        
        _sendQueue = [NSMutableArray array];
        
        self.reconnectDelay = FCTWS_INITIAL_RECONNECT_DELAY;
        self.wsReadyState = FCTWebSocketReadyStateClosed;
        self.webSocket = nil;
    }
    return self;
}

- (RACSubject *)notificationSubject {
    return (RACSubject *)self.notificationSignal;
}

- (void (^)(void))openedBlock {
    NSParameterAssert(_openedBlock);
    return _openedBlock;
}

- (void (^)(NSDictionary *))messageBlock {
    NSParameterAssert(_messageBlock);
    return _messageBlock;
}

- (void)start {
    [self open];
}

#pragma mark - Web Socket Message Sending

- (void)send:(NSString *)message withBackoff:(BOOL)backoff {
    if (self.wsReadyState == FCTWebSocketReadyStateOpened) {
        if (self.sendQueue.count == 0) {
            [self sendBlindly:message];
        } else {
            FCTWSLog(@"Attempt to send with entries in the message queue");
            [self deferSending:message];
        }
    } else {
        [self deferSending:message];
        if (backoff) {
            [self openWithBackoff];
        } else {
            [self open];
        }
    }
}

- (void)sendBlindly:(NSString *)message {
    [self.webSocket send:message];
    FCTWSLog(@"WebSocket sent \"%@\"", message);
}

- (void)deferSending:(NSString *)message {
    [self.sendQueue addObject:message];
    FCTWSLog(@"WebSocket deferred sending \"%@\"", message);
}

#pragma mark - Web Socket Lifecycle

- (void)open {
    if (self.wsReadyState == FCTWebSocketReadyStateClosed) {
        [self initiateOpen];
    }
}

- (void)recover {
    [self close];
    [self openWithBackoff];
}

- (NSTimeInterval)randomJitter {
    // Returns a randon NSTimeInterval between 0 and _reconnectDelay/2 (inclusive).
    NSTimeInterval random = arc4random_uniform(1024) / 1024.;
    return random * (self.reconnectDelay / 2);
}

- (NSString *)randomReqid {
    return [NSString stringWithFormat:@"%@", @(arc4random())];
}

- (void)openWithBackoff {
    // Only try reconnecting for ~20 minutes, cumulatively
    if (self.reconnectDelay < 819.2) {
        NSTimeInterval randomJitter = [self randomJitter];
        NSTimeInterval reconnectDelayPlusRandomJitter = self.reconnectDelay + randomJitter;
        FCTWSLog(@"Using a reconnect delay of %f = %f + %f", reconnectDelayPlusRandomJitter, self.reconnectDelay, randomJitter);
        
        @weakify(self);
        [[[[RACSignal interval:reconnectDelayPlusRandomJitter onScheduler:[RACScheduler mainThreadScheduler]] take:1] takeUntil:self.rac_willDeallocSignal] subscribeNext:^(id x) {
            @strongify(self);
            [self open];
        }];
        
        self.reconnectDelay = MIN(self.reconnectDelay * 2.0, FCTWS_MAX_RECONNECT_DELAY);
    } else {
        FCTWSLog(@"Giving up on web sockets");
    }
}

- (void)initiateOpen {
    self.wsReadyState = FCTWebSocketReadyStateOpening;
    [self finishOpen];
}

- (void)finishOpen {
    if(self.wsReadyState == FCTWebSocketReadyStateOpening) {
        FCTWSLog(@"WebSocket opening");
        NSURLRequest *request = [NSURLRequest requestWithURL:stauntonServerURL()];
        SRWebSocket *webSocket = [[SRWebSocket alloc] initWithURLRequest:request];
        self.webSocket = webSocket;
        self.webSocket.delegate = self;
        [self.webSocket open];
    }
}

- (void)close {
    self.wsReadyState = FCTWebSocketReadyStateClosed;
    // Otherwise SRWebSocket will try to call us (possibly deallocated us) as it closes.
    self.webSocket.delegate = nil;
    FCTWSLog(@"Closing websocket");
    [self.webSocket close];
}

#pragma mark - Function calling

#pragma mark - SRWebSocketDelegate

- (void)webSocket:(SRWebSocket *)webSocket didReceiveMessage:(id)message {
    NSParameterAssert(NSThread.currentThread.isMainThread);
    NSParameterAssert([message isKindOfClass:NSString.class]);
    
    // Ping, pong.
    if ([@"2::" isEqualToString:message]) {
        FCTWSLog(@"Ponging: %@", message);
        [self send:@"2::" withBackoff:NO];
        return;
    }
    
    NSUInteger location = [message rangeOfString:@"{"].location;
    if (location == NSNotFound) {
        FCTWSLog(@"Dropping: %@", message);
        return;
    }
    
    NSString *jsonPartOfMessage = [message substringFromIndex:location];
    NSData *data = [jsonPartOfMessage dataUsingEncoding:NSUTF8StringEncoding];
    NSDictionary *json = [NSJSONSerialization JSONObjectWithData:data options:0 error:nil];
    NSAssert([json isKindOfClass:[NSDictionary class]], @"Invalid JSON returned from WebSocket!");
    
    self.messageBlock(json);
}

- (void)webSocketDidOpen:(SRWebSocket *)webSocket {
    NSParameterAssert(NSThread.currentThread.isMainThread);
    
    // The following test is here to work around what we think is a SocketRocket bug.
    // We have seen crashes in crittercism where we are in webSocketDidOpen but socketrocket is
    // in SR_CONNECTING state, which causes an assertion to fail inside SocketRocket's send method.
    if (self.webSocket.readyState == SR_CONNECTING) {
        NSString *reason = @"webSocketDidOpen was called, but SocketRocket's webSocket.readyState == SR_CONNECTING. To prevent a crash, we are ignoring this call to webSocketDidOpen.";
        FCTWSLog(@"%@", reason);
        return;
    }
    
    self.wsReadyState = FCTWebSocketReadyStateOpened;
    FCTWSLog(@"WebSocket did open");
    for (NSString *message in self.sendQueue) {
        [self sendBlindly:message];
    }
    [self.sendQueue removeAllObjects];
    self.reconnectDelay = FCTWS_INITIAL_RECONNECT_DELAY;
    
    self.openedBlock();
}

// WebSocket errors are not intuitive. Here are the most important cases we're handling here:
//
// * Trello server goes down: didFailWithError
// * Going in and out of cell reception or airplane mode: didFailWithError with POSIX socket error
// * Background the app and then wait a while. Open the app again:
//     didCloseWithCode (!) with wasClean=YES (!!) because Trello server expires our socket
//
// Shrug, computers.

- (void)webSocket:(SRWebSocket *)webSocket didFailWithError:(NSError *)error {
    NSParameterAssert(NSThread.currentThread.isMainThread);
    
    FCTWSLog(@"WebSocket failed with error \"%@\"", error);
    [self recover];
}

- (void)webSocket:(SRWebSocket *)webSocket didCloseWithCode:(NSInteger)code reason:(NSString *)reason wasClean:(BOOL)wasClean {
    NSParameterAssert(NSThread.currentThread.isMainThread);
    
    FCTWSLog(@"WebSocket closed with code \"%@\" and reason \"%@\" and wasClean \"%@\"", @(code), reason, @(wasClean));
    [self recover];
}

@end