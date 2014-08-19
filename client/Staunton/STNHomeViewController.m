#import "STNHomeViewController.h"
#import "STNChessBoardViewController.h"

@interface STNHomeViewController ()

@property (strong, nonatomic) STNChessBoardViewController *boardController;

@end

@implementation STNHomeViewController

- (void )loadView {
    UIView *view = [UIView new];
    self.view = view;
}

- (void)viewDidLoad {
    self.view.frame = [[UIScreen mainScreen] applicationFrame];
    self.boardController = [[STNChessBoardViewController alloc] init];

    CGFloat side = MIN(self.view.frameSizeHeight, self.view.frameSizeWidth);
    self.boardController.view.frame = CGRectMake(0, 0, side, side);
    [self.view addSubview:self.boardController.view];
    [self.boardController didMoveToParentViewController:self];
}

@end
