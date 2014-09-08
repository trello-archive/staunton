//
//  STNWebSocket.h
//  Staunton
//
//  Created by Ian Henry on 9/7/14.
//  Copyright (c) 2014 trello. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "STNDiff.h"

@interface STNWebSocket : NSObject

+ (STNWebSocket *)webSocketWithEmail:(NSString *)email;

@property (nonatomic, readonly, copy) NSString *email;

- (RACSignal *)connectedSignal;

- (RACSignal *)scoreSignal;
- (RACSignal *)kingPositionSignal;
- (RACSignal *)playersPositionSignal; // of STNDiff

- (void)sendMessage:(NSDictionary *)message;

@end
