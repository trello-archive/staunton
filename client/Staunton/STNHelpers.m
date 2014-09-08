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

@implementation RACSignal (STNHelpers)

- (RACSignal *)animated {
    RACSubject *subject = [RACReplaySubject replaySubjectWithCapacity:1];

    [self subscribeNext:^(id x) {
        [UIView animateWithDuration:0.25
                              delay:0
                            options:UIViewAnimationOptionBeginFromCurrentState
                         animations:^{
                             [subject sendNext:x];
                         }
                         completion:nil];
    } error:^(NSError *error) {
        [subject sendError:error];
    } completed:^{
        [subject sendCompleted];
    }];

    return subject;
}

- (RACDisposable *)subscribeLast:(void(^)(id))block {
    __block id val = nil;
    return [self subscribeNext:^(id x) {
        val = x;
    } completed:^{
        block(val);
    }];
}

@end

@implementation UIView (STNHelpers)

- (void)setBorderColor:(UIColor *)borderColor {
    self.layer.borderColor = borderColor.CGColor;
}

- (UIColor *)borderColor {
    return [UIColor colorWithCGColor:self.layer.borderColor];
}

@end