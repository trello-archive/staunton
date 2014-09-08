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
@property (nonatomic, strong) NSSet *allEmails;
@property (nonatomic, strong) RACSubject *playerDiffsSubject;
@property (nonatomic, strong) RACSubject *kingSubject;
@property (nonatomic, strong) RACSubject *scoreSubject;

@end

@implementation STNWebSocket

+ (STNWebSocket *)webSocketWithEmail:(NSString *)email {
    NSParameterAssert(email);
    
    STNWebSocket *this = [[STNWebSocket alloc] init];
    this.email = email;
    this.allEmails = [NSSet set];
    this.playerDiffsSubject = [RACSubject subject];
    this.kingSubject = [RACSubject subject];
    this.scoreSubject = [RACSubject subject];

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
    if (json[@"world"]) {
        [self handleWorldMessage:json[@"world"]];
    }
    if (json[@"king"]) {
        [self handleKingMessage:json[@"king"]];
    }
    if (json[@"scoreboard"]) {
        [self handleScoreMessage:json[@"scoreboard"]];
    }
}

- (void)handleKingMessage:(NSArray *)king {
    [self.kingSubject sendNext:[NSValue valueWithCGPoint:CGPointMake([king[0] floatValue], [king[1] floatValue])]];
}

- (void)handleWorldMessage:(NSArray *)people {
    NSParameterAssert([NSThread isMainThread]);
    // Here there be rampant inefficiencies. Ignore that part.

    NSSet *emails = [NSSet setWithArray:[[[people.rac_sequence map:^(NSArray *diffs) {
        return diffs[0];
    }] filter:^BOOL(NSString *email) {
        return ![email isEqualToString:self.email];
    }] array]];

    NSMutableSet *emailsRemoved = [self.allEmails mutableCopy];
    [emailsRemoved minusSet:emails];

    NSMutableSet *emailsInserted = [emails mutableCopy];
    [emailsInserted minusSet:self.allEmails];

    NSMutableSet *emailsChanged = [emails mutableCopy];
    [emailsChanged minusSet:emailsRemoved];
    [emailsChanged minusSet:emailsInserted];

    CGPoint(^locationFor)(NSString *email) = ^(NSString *email) {
        NSArray *tuple = [[[people.rac_sequence filter:^BOOL(NSArray *tuple) {
            return [tuple[0] isEqualToString:email];
        }] map:^(NSArray *tuple) {
            return tuple[1];
        }] head];
        if (tuple == nil) {
            return CGPointZero;
        } else {
            return CGPointMake([tuple[0] floatValue], [tuple[1] floatValue]);
        }
    };

    for (STNDiff *diff in [[[emailsRemoved.rac_sequence map:^(NSString *email) {
        return [[STNDiffRemove alloc] initWithEmail:email point:CGPointZero];
    }] concat:[emailsInserted.rac_sequence map:^(NSString *email) {
        return [[STNDiffInsert alloc] initWithEmail:email point:locationFor(email)];
    }]] concat:[emailsChanged.rac_sequence map:^(NSString *email) {
        return [[STNDiffUpdate alloc] initWithEmail:email point:locationFor(email)];
    }]]) {
        [self.playerDiffsSubject sendNext:diff];
    }

    self.allEmails = emails;
}

- (void)handleScoreMessage:(NSArray *)score {
    [self.scoreSubject sendNext:[[score.rac_sequence filter:^BOOL(NSArray *tuple) {
        return [tuple[0] isEqualToString:self.email];
    }] head][1]];
}

- (RACSignal *)connectedSignal {
    return self.socket.openedSignal;
}

- (RACSignal *)scoreSignal {
    return self.scoreSubject;
}

- (RACSignal *)kingPositionSignal {
    return self.kingSubject;
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
    return self.playerDiffsSubject;
}

@end
