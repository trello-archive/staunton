//
//  STNHelpers.m
//  Staunton
//
//  Created by Ian Henry on 8/24/14.
//  Copyright (c) 2014 trello. All rights reserved.
//

#import "STNHelpers.h"

CGFloat randfloat() {
    return (CGFloat)arc4random() / 0x100000000;
}

CGPoint CGPointAdd(CGPoint a, CGPoint b) {
    return CGPointMake(a.x + b.x, a.y + b.y);
}

CGPoint CGPointSubtract(CGPoint a, CGPoint b) {
    return CGPointMake(a.x - b.x, a.y - b.y);
}

@implementation RACSignal (Helpers)

- (RACSignal *)animated {
    RACSubject *subject = [RACReplaySubject replaySubjectWithCapacity:1];
    
    [self subscribeNext:^(id x) {
        [UIView animateWithDuration:0.25 animations:^{
            [subject sendNext:x];
        }];
    }];
    
    [self subscribeError:^(NSError *error) {
        [subject sendError:error];
    }];
    
    [self subscribeCompleted:^{
        [subject sendCompleted];
    }];
    
    return subject;
}

@end