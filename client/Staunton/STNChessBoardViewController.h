#import <UIKit/UIKit.h>

@interface STNChessBoardViewController : UIViewController

- (instancetype)initWithDiffSignal:(RACSignal *)diffSignal;

@end
