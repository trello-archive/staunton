//
//  STNWebSocket.m
//  Staunton
//
//  Created by Ian Henry on 9/7/14.
//  Copyright (c) 2014 trello. All rights reserved.
//

#import "STNWebSocket.h"
#import "FCTWebSocket.h"

@interface STNWebSocket ()

@property (nonatomic, strong) FCTWebSocket *socket;
@property (nonatomic, copy, readwrite) NSString *email;

@end

@implementation STNWebSocket

+ (STNWebSocket *)webSocketWithEmail:(NSString *)email {
    NSParameterAssert(email);
    
    STNWebSocket *this = [[STNWebSocket alloc] init];
    this.email = email;
    
    RACSignal *messages = [[this rac_signalForSelector:@selector(sendMessage:)] reduceEach:^(id first) {
        return first;
    }];
    
    this.socket = [[FCTWebSocket alloc] initWithJSONSignal:messages];
    [this handleDisconnects];
    [this.socket start];
    
    return this;
}

- (void)handleDisconnects {
    @weakify(self);
    [[[self.socket.openedSignal ignore:@NO] take:1] subscribeCompleted:^{
        @strongify(self);
        [self sendMessage:@{@"email": self.email}];
        
        RACSignal *onDisconnect = [[self.socket.openedSignal ignore:@YES] take:1];
        
        [[self.socket.messageSignal takeUntil:onDisconnect] subscribeNext:^(NSDictionary *json) {
            if (json[@"ping"]) {
                [self sendMessage:@{@"pong": @YES}];
            } else {
                [self handleMessage:json];
            }
        }];
        
        [onDisconnect subscribeCompleted:^{
            [self handleDisconnects];
        }];
    }];
}

- (void)handleMessage:(NSDictionary *)json {
    NSLog(@"%@", json);
}

- (RACSignal *)connectedSignal {
    return self.socket.openedSignal;
}

- (RACSignal *)scoreSignal {
    return nil;
}

- (RACSignal *)kingPositionSignal {
    return nil;
}

- (void)sendMessage:(NSDictionary *)message {
    // We define this method, but it has no implementation.
    // What's up with that? Well, one of the cool things
    // RAC does is let us do some pretty heavy swizzling
    // magic. This is a... debatable practice in production
    // code, as it can be a little confusing to future
    // maintainers. Here, it lets us express the JSONSignal
    // we're passing to FCTWebSocket quite nicely, and
    // gives as an excuse to show it off.
}

- (RACSignal *)playersPositionSignal {
    NSString *hao = @"me@haolian.org";
    NSString *ian = @"foo@bar.com";
    
    return [[[@[[[STNDiffInsert alloc] initWithEmail:hao point:CGPointMake(0.5, 0.25)],
                [[STNDiffInsert alloc] initWithEmail:ian point:CGPointMake(0.5, 0.75)]]
              rac_sequence] signalWithScheduler:[RACScheduler mainThreadScheduler]]
            concat: [[RACSignal interval:2 onScheduler:[RACScheduler mainThreadScheduler]] map:^(id x) {
        NSString *email = arc4random_uniform(2) ? hao : ian;
        CGPoint point = CGPointMake(randfloat(), randfloat());
        return [[STNDiffUpdate alloc] initWithEmail:email point:point];
    }]];
}

@end
