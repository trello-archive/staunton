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

- (UILongPressGestureRecognizer *)addRecognizer:(UIView *)view {
    UILongPressGestureRecognizer *recognizer = [[UILongPressGestureRecognizer alloc] initWithTarget:nil action:nil];
    recognizer.minimumPressDuration = 0.1;
    view.userInteractionEnabled = YES;
    [view addGestureRecognizer:recognizer];
    return recognizer;
}

- (RACSignal *)centerSignalFromRecognizer:(UILongPressGestureRecognizer *)recognizer {
    RACSubject *centerSubject = [RACSubject subject];
    __block CGPoint initialPosition;
    [recognizer.rac_gestureSignal subscribeNext:^(UILongPressGestureRecognizer *recognizer) {
        UIView *view = recognizer.view;
        switch (recognizer.state) {
            case UIGestureRecognizerStateBegan:
                initialPosition = [recognizer locationInView:view];
                break;
            case UIGestureRecognizerStateChanged: {
                CGPoint topLeft = CGPointSubtract([recognizer locationInView:view.superview], initialPosition);
                CGPoint center = CGPointAdd(topLeft, CGPointMake(CGRectGetMidX(view.bounds), CGRectGetMidY(view.bounds)));
                [centerSubject sendNext:[NSValue valueWithCGPoint:center]];
                break;
            }
            case UIGestureRecognizerStateEnded:
                // blah blah, send update here
                break;
            default: break;
        };
    }];
    return centerSubject;
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
        
        UILongPressGestureRecognizer *recognizer = [self addRecognizer:gravatarView];
        
        RACSignal *isDragging = [RACObserve(recognizer, state) map:^id(NSNumber *stateNumber) {
            return @(stateNumber.unsignedIntegerValue == UIGestureRecognizerStateBegan
            || stateNumber.unsignedIntegerValue == UIGestureRecognizerStateChanged);
        }];
        
        RAC(gravatarView, center) = [RACSignal if:isDragging
                                     then:[self centerSignalFromRecognizer:recognizer]
                                     else:[RACSignal defer:^{
            return [[self centerForPosition:positionSignal] animated];
        }]];
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
