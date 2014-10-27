#import "STNChessBoardViewController.h"
#import "STNChessBoardView.h"
#import "STNDiff.h"
#import "STNViewFactory.h"

@interface STNChessBoardViewController ()

@property (strong, nonatomic) UIView *myView;
@property (strong, nonatomic) UIView *kingView;
@property (strong, nonatomic) STNWebSocket *socket;

@end

static CGFloat distanceBetween(CGPoint a, CGPoint b) {
    return sqrtf(powf(a.x - b.x, 2) +
                 powf(a.y - b.y, 2));
}

@implementation STNChessBoardViewController

- (instancetype)initWithSocket:(STNWebSocket *)socket {
    NSParameterAssert(socket);
    if (self = [super init]) {
        self.socket = socket;
    }
    return self;
}

- (void)loadView {
    self.view = [[STNChessBoardView alloc] init];
}

- (CGFloat)gravatarSize {
    return self.view.bounds.size.width * 0.15;
}

- (void)viewWillAppear:(BOOL)animated {
    [super viewWillAppear:animated];

    [self prepareMyView];
    [[[RACObserve(self.socket, kingPosition) skip:1] take:1] subscribeCompleted:^{
        [self prepareKingView];
    }];

    @weakify(self);
    [[self.socket.playersPositionSignal groupBy:^(STNDiff *diff) {
        return diff.email;
    }] subscribeNext:^(RACGroupedSignal *perUserDiffs) {
        @strongify(self);
        [self handleDiffs:perUserDiffs forEmail:(NSString *)perUserDiffs.key];
    }];
}

- (void)prepareKingView {
    CGFloat size = [self gravatarSize] * 1.2;
    self.kingView = [STNViewFactory makeKingViewWithSize:size];
    [self.view insertSubview:self.kingView belowSubview:self.myView];
    RAC(self.kingView, center) = [[self relativeToAbsolute:RACObserve(self.socket, kingPosition)] animated];
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

- (UILabel *)scoreLabelWithFrame:(CGRect)frame {
    UILabel *scoreLabel = [STNViewFactory makeScoreLabelWithFrame:frame];
#warning Exercise 1
    // Let's make this dynamically update!
    scoreLabel.text = @"???";
    return scoreLabel;
}

- (void)prepareMyView {
    UIImageView *gravatarView = [STNViewFactory makeGravatarViewWithSize:self.gravatarSize email:self.socket.email];

    UIView *view = [[UIView alloc] initWithFrame:gravatarView.frame];
    view.center = CGPointMake(CGRectGetMidX(self.view.bounds), CGRectGetMidY(self.view.bounds));
    [view addSubview:gravatarView];

    [view addSubview:[self scoreLabelWithFrame:CGRectMake(0, 0, view.bounds.size.width, 20)]];

    UILongPressGestureRecognizer *recognizer = [self addRecognizer:view];
    RACSignal *dragSignal = [self signalForRecognizer:recognizer];
    RACSignal *isDragging = [self isDraggingSignal:dragSignal];

#warning Exercise 2
    // It would be cool if this showed a gray border when we were offline.
    gravatarView.borderColor = [UIColor whiteColor];
    gravatarView.layer.borderWidth = 2;

    RAC(gravatarView.layer, shadowOffset) = [RACSignal if:isDragging
                                             then:[RACSignal return:[NSValue valueWithCGSize:CGSizeMake(2, 2)]]
                                             else:[RACSignal return:[NSValue valueWithCGSize:CGSizeMake(0, 0)]]];
    RAC(gravatarView.layer, shadowRadius) = [RACSignal if:isDragging
                                             then:[RACSignal return:@5]
                                             else:[RACSignal return:@1]];

    @weakify(self);
    [dragSignal subscribeNext:^(RACSignal *drag) {
        [drag subscribeLast:^(NSValue *center) {
            @strongify(self);
            if (center) {
                CGPoint relative = [self absoluteToRelative:center.CGPointValue];
                [self.socket sendMessage:@{@"x": @(relative.x), @"y": @(relative.y)}];
            }
        }];
    }];

    UILabel *distanceLabel = [STNViewFactory makeScoreLabelWithFrame:CGRectMake(0, CGRectGetHeight(view.bounds) - 20, view.bounds.size.width, 20)];
    distanceLabel.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleTopMargin;
    [view addSubview:distanceLabel];

#warning Exercise 3
    // Alright, so we know that we get more points the closer we are to the king. But...
    // how close are we to the king? Let's make this label update with our distance!
    distanceLabel.text = @"";

#warning Exercise 4
    // See Ian.

#warning Exercise 5 (mad extra credit)
    // This shouldn't let you drag outside the chessboard!
    RAC(view, center) = [dragSignal switchToLatest];

    self.myView = view;
    [self.view addSubview:self.myView];
}

- (CGPoint)absoluteToRelative:(CGPoint)absolute {
    return CGPointMake(absolute.x / self.view.bounds.size.width,
                       absolute.y / self.view.bounds.size.height);
}

- (RACSignal *)relativeToAbsolute:(RACSignal *)positionSignal {
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

    [insertions subscribeNext:^(STNDiff *insertion) {
        NSParameterAssert([NSThread isMainThread]);
        if (gravatarView != nil) {
            NSLog(@"dropping insertion of known email!");
            return;
        }
        gravatarView = [STNViewFactory makeGravatarViewWithSize:self.gravatarSize email:email];
        [self.view insertSubview:gravatarView belowSubview:self.myView];

        RACSignal *positionSignal = [[[diffs takeUntil:removals] map:^(STNDiff *diff) {
            return [NSValue valueWithCGPoint:diff.point];
        }] startWith:[NSValue valueWithCGPoint:insertion.point]];

        RAC(gravatarView, center) = [[self relativeToAbsolute:positionSignal] animated];
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
