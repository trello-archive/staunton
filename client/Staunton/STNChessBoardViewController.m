#import "STNChessBoardViewController.h"
#import "STNChessBoardView.h"
#import "STNDiff.h"

@interface STNChessBoardViewController ()

@property (strong, nonatomic) RACSignal *diffSignal;
@property (strong, nonatomic) UIView *myView;
@property (strong, nonatomic) UIView *kingView;

@end

static UIImageView *makeGravatarView(CGFloat size) {
    UIImageView *view = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, size, size)];
    view.layer.cornerRadius = 0;
    view.layer.masksToBounds = NO;
    view.layer.shadowColor = [UIColor blackColor].CGColor;
    view.layer.shadowOffset = CGSizeZero;
    view.layer.shadowOpacity = 0.5;
    view.layer.shadowRadius = 2;
    view.layer.shadowPath = [UIBezierPath bezierPathWithRoundedRect:view.bounds cornerRadius:view.layer.cornerRadius].CGPath;
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

- (CGFloat)gravatarSize {
    return self.view.bounds.size.width * 0.06;
}

- (void)viewWillAppear:(BOOL)animated {
    [super viewWillAppear:animated];

    [self prepareMyView];
    [self prepareKingView];

    @weakify(self);
    [[self.diffSignal groupBy:^(STNDiff *diff) {
        return diff.email;
    }] subscribeNext:^(RACGroupedSignal *perUserDiffs) {
        @strongify(self);
        [self handleDiffs:perUserDiffs forEmail:(NSString *)perUserDiffs.key];
    }];
}

- (void)prepareKingView {
    UILabel *label = [[UILabel alloc] initWithFrame:CGRectMake(0, 0, self.gravatarSize, self.gravatarSize)];
    label.text = @"♚";
    label.font = [UIFont systemFontOfSize:self.gravatarSize];
    label.backgroundColor = [UIColor redColor];
    label.textAlignment = NSTextAlignmentCenter;
    [self.view addSubview:label];
    self.kingView = label;
}

- (RACSignal *)isDraggingSignal:(RACSignal *)dragSignal {
    RACSubject *isDragging = [RACReplaySubject replaySubjectWithCapacity:1];
    [isDragging sendNext:@NO];
    [dragSignal subscribeNext:^(RACSignal *inner) {
        [isDragging sendNext:@YES];
        [inner subscribeCompleted:^{
            [isDragging sendNext:@NO];
        }];
    }];
    return isDragging;
}

- (void)prepareMyView {
    UIImageView *gravatarView = makeGravatarView(self.gravatarSize);

    UILongPressGestureRecognizer *recognizer = [self addRecognizer:gravatarView];
    RACSignal *dragSignal = [self signalForRecognizer:recognizer];
    RACSignal *isDragging = [self isDraggingSignal:dragSignal];

    [gravatarView setImageWithGravatarEmailAddress:@"foo@bar.com"];

    gravatarView.layer.borderColor = UIColor.whiteColor.CGColor;

    RAC(gravatarView.layer, shadowOffset) = [RACSignal if:isDragging
                                             then:[RACSignal return:[NSValue valueWithCGSize:CGSizeMake(2, 2)]]
                                             else:[RACSignal return:[NSValue valueWithCGSize:CGSizeMake(0, 0)]]];
    RAC(gravatarView.layer, shadowRadius) = [RACSignal if:isDragging
                                             then:[RACSignal return:@5]
                                             else:[RACSignal return:@1]];

    RAC(gravatarView, center) = [dragSignal switchToLatest];

    [self.view addSubview:gravatarView];
    self.myView = gravatarView;
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

- (UILongPressGestureRecognizer *)addRecognizer:(UIView *)view {
    UILongPressGestureRecognizer *recognizer = [[UILongPressGestureRecognizer alloc] initWithTarget:nil action:nil];
    recognizer.minimumPressDuration = 0.1;
    view.userInteractionEnabled = YES;
    [view addGestureRecognizer:recognizer];
    return recognizer;
}

// Here's how I chose to model the drag and drop:
//
// The recognizer has a signal of "drags." This isn't a position!
// Every time a touch begins, it sends a new signal with positions for the touch.
// Every time a touch ends, that inner signal completes.
// So dragging is a *signal of signals*.
// This is more useful than just having a signal of positions, as
// it lets us track when each drag event starts and stops.
- (RACSignal *)signalForRecognizer:(UILongPressGestureRecognizer *)recognizer {
    RACSubject *dragSubject = [RACSubject subject];

    __block RACSubject *centerSubject;
    __block CGPoint initialPosition;

    [recognizer.rac_gestureSignal subscribeNext:^(UILongPressGestureRecognizer *recognizer) {
        UIView *view = recognizer.view;
        switch (recognizer.state) {
            case UIGestureRecognizerStateBegan:
                initialPosition = [recognizer locationInView:view];
                centerSubject = [RACSubject subject];
                [dragSubject sendNext:centerSubject];
                // (deliberately falling through)
            case UIGestureRecognizerStateChanged: {
                CGPoint topLeft = CGPointSubtract([recognizer locationInView:view.superview], initialPosition);
                CGPoint center = CGPointAdd(topLeft, CGPointMake(CGRectGetMidX(view.bounds), CGRectGetMidY(view.bounds)));
                [centerSubject sendNext:[NSValue valueWithCGPoint:center]];
                break;
            }
            case UIGestureRecognizerStateEnded:
                [centerSubject sendCompleted];
                break;
            default: break;
        };
    }];

    return dragSubject;
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
        gravatarView = makeGravatarView(self.gravatarSize);
        [gravatarView setImageWithGravatarEmailAddress:email];
        [self.view addSubview:gravatarView];

        RACSignal *positionSignal = [[diffs takeUntil:removals] map:^(STNDiff *diff) {
            return [NSValue valueWithCGPoint:diff.point];
        }];

        RAC(gravatarView, center) = [[self centerForPosition:positionSignal] animated];
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

@end
