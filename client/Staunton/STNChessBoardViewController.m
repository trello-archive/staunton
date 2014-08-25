#import "STNChessBoardViewController.h"
#import "STNChessBoardView.h"
#import "STNDiff.h"

@interface STNChessBoardViewController ()

@property (strong, nonatomic) NSMutableDictionary *gravatarViewsByEmail;
@property (strong, nonatomic) RACSignal *diffsSignal;

@end

@implementation STNChessBoardViewController

static const NSUInteger gravatarSide = 32;

+ (UIImageView *)makeGravatarViewWithFrame:(CGRect)frame {
    UIImageView *view = [[UIImageView alloc] initWithFrame:frame];
    UIBezierPath *shadowPath = [UIBezierPath bezierPathWithRect:view.bounds];
    view.layer.masksToBounds = NO;
    view.layer.shadowColor = [UIColor blackColor].CGColor;
    view.layer.shadowOffset = CGSizeMake(2, 2);
    view.layer.shadowOpacity = 0.5;
    view.layer.shadowPath = shadowPath.CGPath;
    return view;
}

- (instancetype)initWithDiffsSignal:(RACSignal *)diffsSignal {
    if (self = [super init]) {
        self.gravatarViewsByEmail = [NSMutableDictionary dictionary];
        self.diffsSignal = diffsSignal;
    }
    return self;
}

- (void)loadView {
    self.view = [[STNChessBoardView alloc] init];
}

- (void)viewDidLoad {
    @weakify(self);

    [self.diffsSignal subscribeNext:^(STNDiff *diff) {
        @strongify(self);
        [diff visitWithInsertBlock:^(STNDiffInsert *insert) {
            UIImageView *gravatarView = [self.class makeGravatarViewWithFrame:CGRectMake(insert.point.x, insert.point.y, gravatarSide, gravatarSide)];
            [gravatarView setImageWithGravatarEmailAddress:insert.email];
            [self.view addSubview:gravatarView];
            self.gravatarViewsByEmail[insert.email] = gravatarView;
        } removeBlock:^(STNDiffRemove *remove) {
            UIImageView *gravatarView = self.gravatarViewsByEmail[remove.email];
            NSParameterAssert(gravatarView);
            [gravatarView removeFromSuperview];
            [self.gravatarViewsByEmail removeObjectForKey:gravatarView];
        } updateBlock:^(STNDiffUpdate *update) {
            UIImageView *gravatarView = self.gravatarViewsByEmail[update.email];
            NSParameterAssert(gravatarView);
            gravatarView.frameOrigin = update.point;
        }];
    }];
}

@end
