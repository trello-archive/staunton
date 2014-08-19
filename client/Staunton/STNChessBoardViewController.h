#import <UIKit/UIKit.h>

@interface STNChessBoardViewController : UIViewController

- (instancetype)initWithDiffsSignal:(RACSignal *)diffsSignal;

@end
