#import "STNHomeViewController.h"
#import "STNChessBoardViewController.h"
#import "STNDiff.h"
#import "FCTWebSocket.h"

@interface STNHomeViewController ()

@property (strong, nonatomic) STNChessBoardViewController *boardController;
@property (strong, nonatomic) UILabel *label;
@property (strong, nonatomic) FCTWebSocket *socket;

@end

@implementation STNHomeViewController

- (void)viewDidLoad {
    [self prepareSocket];
}

- (void)prepareSocket {
    self.label = [[UILabel alloc] init];
    self.label.font = [UIFont fontWithName:@"Futura" size:15];

    self.socket = [[FCTWebSocket alloc] init];

    /// EXERCISE ONE: HELLO, REACTIVE COCOA
    ///
    /// As self.socket.openedSignal sends you @YESes and @NOs, please
    /// reactively update self.label.text. This will give you a fun and,
    /// more importantly, useful indicator of the server status.
    ///
    /// Solution: git stash; git co 1

    RAC(self, label.text) = [[self.socket.openedSignal map:^id(NSNumber *n) {
        return n.boolValue ? @"+ WebSocket: connected ∰." : @"+ WebSocket: disconnected ☁.";
    }] startWith:@"+ WebSocket: initializing..."];
}

- (void)viewWillAppear:(BOOL)animated {
    [super viewWillAppear:animated];
    RACSignal *diffSignal = [self fakeDiffSignal];
    self.boardController = [[STNChessBoardViewController alloc] initWithDiffSignal:diffSignal];
    
    CGFloat side = MIN(self.view.frameSizeHeight, self.view.frameSizeWidth);
    self.boardController.view.frame = CGRectMake(0, [UIApplication sharedApplication].statusBarFrame.size.height, side, side);
    [self.view addSubview:self.boardController.view];
    [self.boardController didMoveToParentViewController:self];

    CGFloat bottom = self.boardController.view.frameOriginY + self.boardController.view.frameSizeHeight;
    self.label.frame = CGRectMake(0, bottom, side, side);
    [self.view addSubview:self.label];
    self.label.frameSizeHeight = [self.label sizeThatFits:CGSizeMake(side, CGFLOAT_MAX)].height;

    [self.socket start];
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
