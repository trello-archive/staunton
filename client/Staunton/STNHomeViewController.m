#import "STNHomeViewController.h"

@interface STNHomeViewController ()

@end

@implementation STNHomeViewController

- (void )loadView {
    UIView *view = [UIView new];
    view.layer.borderColor = [UIColor redColor].CGColor;
    view.layer.borderWidth = 1;
    self.view = view;
}

- (void)viewDidLoad {
    self.view.frame = [[UIScreen mainScreen] applicationFrame];
}

@end
