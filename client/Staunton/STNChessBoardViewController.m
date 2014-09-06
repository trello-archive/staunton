#import "STNChessBoardViewController.h"
#import "STNChessBoardView.h"
#import "STNDiff.h"

@interface STNChessBoardViewController ()

@property (strong, nonatomic) RACSignal *diffSignal;

@end

static UIImageView *makeGravatarView(CGFloat size) {
    UIImageView *view = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, size, size)];
    UIBezierPath *shadowPath = [UIBezierPath bezierPathWithRect:view.bounds];
    view.layer.masksToBounds = NO;
    view.layer.shadowColor = [UIColor blackColor].CGColor;
    view.layer.shadowOffset = CGSizeMake(2, 2);
    view.layer.shadowOpacity = 0.5;
    view.layer.shadowPath = shadowPath.CGPath;
    return view;
}

@implementation STNChessBoardViewController

- (instancetype)initWithDiffSignal:(RACSignal *)diffSignal {
    if (self = [super init]) {
        self.diffSignal = diffSignal;
    }
    return self;
}

- (void)loadView {
    self.view = [[STNChessBoardView alloc] init];
}

- (RACSignal *)centerForPosition:(RACSignal *)positionSignal {
    return [RACSignal combineLatest:@[RACObserve(self.view, bounds),
                                      positionSignal]
                             reduce:^(NSValue *boundsValue, NSValue *positionValue) {
                                 CGRect bounds = [boundsValue CGRectValue];
                                 CGPoint position = [positionValue CGPointValue];
                                 return [NSValue valueWithCGPoint:CGPointMake(bounds.size.width * position.x,
                                                                              bounds.size.height * position.y)];
                             }];
}

- (void)handleDiffs:(RACSignal *)diffs forEmail:(NSString *)email {
    __block UIImageView *gravatarView = nil;
    
    RACSignal *insertions = [diffs filter:^BOOL(STNDiff *diff) {
        return diff.isInsert;
    }];

    RACSignal *removals = [diffs filter:^BOOL(STNDiff *diff) {
        return diff.isRemove;
    }];
    
    [insertions subscribeNext:^(id x) {
        if (gravatarView != nil) {
            NSLog(@"dropping insertion of known email!");
            return;
        }
        gravatarView = makeGravatarView(self.view.bounds.size.width * 0.06);
        [gravatarView setImageWithGravatarEmailAddress:email];
        [self.view addSubview:gravatarView];

        RACSignal *positionSignal = [[diffs takeUntil:removals] map:^(STNDiff *diff) {
            return [NSValue valueWithCGPoint:diff.point];
        }];
        
        RAC(gravatarView, center) = [self centerForPosition:positionSignal];
        [positionSignal subscribeCompleted:^{
            [gravatarView removeFromSuperview];
        }];
    }];
    
    [removals subscribeNext:^(id x) {
        if (gravatarView == nil) {
            NSLog(@"dropping removal of unknown email!");
            return;
        }
        [gravatarView removeFromSuperview];
        gravatarView = nil;
    }];
}

- (void)viewWillAppear:(BOOL)animated {
    [super viewWillAppear:animated];
    
    @weakify(self);
    [[self.diffSignal groupBy:^(STNDiff *diff) {
        return diff.email;
    }] subscribeNext:^(RACGroupedSignal *perUserDiffs) {
        @strongify(self);
        [self handleDiffs:perUserDiffs forEmail:(NSString *)perUserDiffs.key];
    }];
}

@end
