#import "STNChessBoardView.h"

@implementation STNChessBoardView

static const NSUInteger STNChessSize = 8;

void CGContextSetNiceDarkBrownColor(CGContextRef context) {
    CGContextSetRGBFillColor(context, 231. / 255, 171. / 255, 87. / 255, 1.0);
}

void CGContextSetDelightfulLightBrownColor(CGContextRef context) {
    CGContextSetRGBFillColor(context, 147. / 255, 69. / 255, 20. / 255, 1.0);
}

- (void)drawRect:(CGRect)rect {
    [super drawRect:rect];
    CGFloat width = self.bounds.size.width / STNChessSize;
    CGFloat height = self.bounds.size.height / STNChessSize;

    for (NSUInteger row = 0; row < 16; row++) {
        for (NSUInteger col = 0; col < 16; col++) {
            CGRect rectangle = CGRectMake(row * width, col * height, width, height);
            CGContextRef context = UIGraphicsGetCurrentContext();
            if ((row + col) % 2 == 0) {
                CGContextSetNiceDarkBrownColor(context);
            } else {
                CGContextSetDelightfulLightBrownColor(context);
            }
            CGContextFillRect(context, rectangle);
        }
    }
}

@end
