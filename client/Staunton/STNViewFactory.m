//
//  STNViewFactory.m
//  Staunton
//
//  Created by Hao on 9/8/14.
//  Copyright (c) 2014 trello. All rights reserved.
//

#import "STNViewFactory.h"

@implementation STNViewFactory

+ (UIImageView *)makeGravatarViewWithSize:(CGFloat)size email:(NSString *)email {
    UIImageView *view = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, size, size)];
    [view setImageWithGravatarEmailAddress:email
                          placeholderImage:nil
                          defaultImageType:KHGravatarDefaultImageRetro
                              forceDefault:NO
                                    rating:KHGravatarRatingR];
    view.layer.cornerRadius = 0;
    view.layer.masksToBounds = NO;
    view.layer.shadowColor = [UIColor blackColor].CGColor;
    view.layer.shadowOffset = CGSizeZero;
    view.layer.shadowOpacity = 0.5;
    view.layer.shadowRadius = 2;
    view.layer.shadowPath = [UIBezierPath bezierPathWithRoundedRect:view.bounds cornerRadius:view.layer.cornerRadius].CGPath;
    return view;
}

+ (UILabel *)makeKingViewWithSize:(CGFloat)size {
    UILabel *label = [[UILabel alloc] initWithFrame:CGRectMake(0, 0, size, size)];
    label.text = @"♚";
    label.font = [UIFont systemFontOfSize:size];
    label.backgroundColor = [UIColor whiteColor];
    label.layer.cornerRadius = size * 0.5;
    label.layer.borderWidth = 2;
    label.layer.borderColor = UIColor.blackColor.CGColor;
    label.layer.masksToBounds = YES;
    label.textAlignment = NSTextAlignmentCenter;
    return label;
}

@end
