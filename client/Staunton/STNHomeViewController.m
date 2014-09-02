#import "STNHomeViewController.h"
#import "STNChessBoardViewController.h"
#import "STNDiff.h"

@interface STNHomeViewController ()

@property (strong, nonatomic) STNChessBoardViewController *boardController;

@end

@implementation STNHomeViewController

- (void)viewWillAppear:(BOOL)animated {
    [super viewWillAppear:animated];
    RACSignal *diffSignal = [self fakeDiffSignal];
    self.boardController = [[STNChessBoardViewController alloc] initWithDiffSignal:diffSignal];
    
    CGFloat side = MIN(self.view.frameSizeHeight, self.view.frameSizeWidth);
    self.boardController.view.frame = CGRectMake(0, 0, side, side);
    [self.view addSubview:self.boardController.view];
    [self.boardController didMoveToParentViewController:self];
}

- (RACSignal *)fakeDiffSignal {
    NSString *hao = @"me@haolian.org";
    NSString *ian = @"ianthehenry@gmail.com";
    
    return [[[@[[[STNDiffInsert alloc] initWithEmail:hao point:CGPointMake(0.5, 0.25)],
                [[STNDiffInsert alloc] initWithEmail:ian point:CGPointMake(0.5, 0.75)]]
              rac_sequence] signalWithScheduler:[RACScheduler mainThreadScheduler]]
            concat: [[RACSignal interval:2 onScheduler:[RACScheduler mainThreadScheduler]] map:^(id x) {
        NSString *email = arc4random_uniform(2) ? hao : ian;
        CGPoint point = CGPointMake(randfloat(), randfloat());
        return [[STNDiffUpdate alloc] initWithEmail:email point:point];
    }]];
}

@end
