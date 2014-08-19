#import "STNHomeViewController.h"
#import "STNChessBoardViewController.h"
#import "STNDiff.h"

@interface STNHomeViewController ()

@property (strong, nonatomic) STNChessBoardViewController *boardController;

@end

@implementation STNHomeViewController

- (void )loadView {
    UIView *view = [UIView new];
    self.view = view;
}

- (void)viewWillAppear:(BOOL)animated {
    RACSubject *diffsSignal = [RACSubject subject];
    self.boardController = [[STNChessBoardViewController alloc] initWithDiffsSignal:diffsSignal];

    CGFloat side = MIN(self.view.frameSizeHeight, self.view.frameSizeWidth);
    self.boardController.view.frame = CGRectMake(0, 0, side, side);
    [self.view addSubview:self.boardController.view];
    [self.boardController didMoveToParentViewController:self];

    [self sendFakeDataTo:diffsSignal];
}

- (void)sendFakeDataTo:(RACSubject *)diffsSignal {
    NSString *hao = @"me@haolian.org";
    NSString *ian = @"ianthehenry@gmail.com";
    [diffsSignal sendNext:[[STNDiffInsert alloc] initWithEmail:hao point:CGPointMake(100, 100)]];
    [diffsSignal sendNext:[[STNDiffInsert alloc] initWithEmail:ian point:CGPointMake(150, 150)]];
    [[RACSignal interval:2 onScheduler:[RACScheduler mainThreadScheduler]] subscribeNext:^(id x) {
        NSString *email = arc4random_uniform(2) ? hao : ian;
        CGPoint point = CGPointMake(arc4random_uniform(self.boardController.view.frameSizeWidth), arc4random_uniform(self.boardController.view.frameSizeHeight));
        [diffsSignal sendNext:[[STNDiffUpdate alloc] initWithEmail:email point:point]];
    }];
}

@end
