//
//  STNViewFactory.h
//  Staunton
//
//  Created by Hao on 9/8/14.
//  Copyright (c) 2014 trello. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface STNViewFactory : NSObject

+ (UIImageView *)makeGravatarViewWithSize:(CGFloat)size email:(NSString *)email;
+ (UILabel *)makeKingViewWithSize:(CGFloat)size;
+ (UILabel *)makeScoreLabelWithFrame:(CGRect)frame;

@end
