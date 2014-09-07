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

static const NSUInteger FCT_DEBUG_WEB_SOCKETS = 1;

static void FCTWSLog(NSString *format, ...) {
    if (FCT_DEBUG_WEB_SOCKETS) {
        va_list args;
        va_start(args, format);
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wformat-nonliteral"
        NSLogv([NSString stringWithFormat:@"\nFCTWSLog: %@", format], args);
#pragma clang diagnostic pop
        va_end(args);
    }
}

static NSURL *stauntonServerURL(void) {
    return [NSURL URLWithString:@"ws://localhost:9160/"];
}

typedef enum {
    FCTWebSocketReadyStateOpened,
    FCTWebSocketReadyStateOpening,
    FCTWebSocketReadyStateClosed
} FCTWebSocketReadyState;

#pragma mark - Class extension

@interface FCTWebSocket () <SRWebSocketDelegate>

@property (nonatomic, strong) SRWebSocket *webSocket;

// For messages that haven't been sent (because the socket was closed).
@property (nonatomic, strong, readonly) NSMutableArray *sendQueue;

@property (nonatomic, assign) FCTWebSocketReadyState wsReadyState;

@property (nonatomic, strong) RACSubject *messageSubject;
@property (nonatomic, strong) RACSubject *openedSubject;


@end

@implementation FCTWebSocket

- (void)dealloc {
    [self.messageSubject sendCompleted];
    [self close];
}

- (instancetype)init {
    if (self = [super init]) {
        FCTWSLog(@"Protip: Use initWithJSONSignal instead to send JSON (Exercise 2)");
        [self prepare];
    }
    return self;
}

- (instancetype)initWithJSONSignal:(RACSignal *)JSONSignal {
    if (self = [super init]) {
        @weakify(self);
        [self prepare];
        [[JSONSignal takeUntil:self.rac_willDeallocSignal] subscribeNext:^(id x) {
            @strongify(self);
            NSData *data = [NSJSONSerialization dataWithJSONObject:x options:0 error:nil];
            NSString *unicode = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
            [self send:unicode withBackoff:YES];
        }];
    }
    return self;
}

- (void)prepare {
    _sendQueue = [NSMutableArray array];
    _messageSubject = [RACSubject subject];
    _openedSubject = [RACSubject subject];
    
    self.wsReadyState = FCTWebSocketReadyStateClosed;
    self.webSocket = nil;
}

- (RACSignal *)messageSignal {
    NSParameterAssert(self.messageSubject);
    return self.messageSubject;
}

- (RACSignal *)openedSignal {
    NSParameterAssert(self.openedSubject);
    return [self.openedSubject distinctUntilChanged];
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

- (void)openWithBackoff {
    @weakify(self);
    [[[[RACSignal interval:1 onScheduler:[RACScheduler mainThreadScheduler]] take:1] takeUntil:self.rac_willDeallocSignal] subscribeNext:^(id x) {
        @strongify(self);
        [self open];
    }];
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

    FCTWSLog(@"WebSocket received %@", jsonPartOfMessage);
    [self.messageSubject sendNext:json];
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
    [self.openedSubject sendNext:@YES];
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
    [self.openedSubject sendNext:@NO];
    [self recover];
}

- (void)webSocket:(SRWebSocket *)webSocket didCloseWithCode:(NSInteger)code reason:(NSString *)reason wasClean:(BOOL)wasClean {
    NSParameterAssert(NSThread.currentThread.isMainThread);
    
    FCTWSLog(@"WebSocket closed with code \"%@\" and reason \"%@\" and wasClean \"%@\"", @(code), reason, @(wasClean));
    [self.openedSubject sendNext:@NO];
    [self recover];
}

@end