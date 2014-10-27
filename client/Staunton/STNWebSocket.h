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
@property (nonatomic, readonly, assign) BOOL connected;
@property (nonatomic, readonly, assign) double score;
@property (nonatomic, readonly, copy) NSOrderedSet *scores;
@property (nonatomic, readonly, assign) CGPoint kingPosition;

- (RACSignal *)playersPositionSignal; // of STNDiff

- (void)sendMessage:(NSDictionary *)message;

@end
