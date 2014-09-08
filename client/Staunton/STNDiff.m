#import "STNDiff.h"

@interface STNDiff ()

@property (nonatomic, strong) NSString *email;
@property (nonatomic, assign) CGPoint point;

@end

@implementation STNDiff

- (instancetype)initWithEmail:(NSString *)email point:(CGPoint)point {
    if (self = [super init]) {
        self.email = email;
        self.point = point;
    }
    return self;
}

- (BOOL)isInsert {
    return NO;
}

- (BOOL)isUpdate {
    return NO;
}

- (BOOL)isRemove {
    return NO;
}

@end

@implementation STNDiffInsert

- (BOOL)isInsert {
    return YES;
}

@end

@implementation STNDiffRemove

- (BOOL)isRemove {
    return YES;
}

@end

@implementation STNDiffUpdate

- (BOOL)isUpdate {
    return YES;
}

@end