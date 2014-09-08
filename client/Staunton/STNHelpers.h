//
//  STNHelpers.h
//  Staunton
//
//  Created by Ian Henry on 8/24/14.
//  Copyright (c) 2014 trello. All rights reserved.
//

CGFloat randfloat();
CGPoint CGPointAdd(CGPoint a, CGPoint b);
CGPoint CGPointSubtract(CGPoint a, CGPoint b);

@interface UIView (STNHelpers)

@property (nonatomic, copy) UIColor *borderColor;

@end


@interface RACSignal (STNHelpers)

- (RACSignal *)animated;
- (RACDisposable *)subscribeLast:(void(^)(id))block;

@end