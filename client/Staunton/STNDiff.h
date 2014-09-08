#import <Foundation/Foundation.h>

@class STNDiffInsert, STNDiffRemove, STNDiffUpdate;

@interface STNDiff : NSObject

@property (nonatomic, readonly) NSString *email;
@property (nonatomic, readonly) CGPoint point;
@property (nonatomic, readonly) BOOL isInsert, isUpdate, isRemove;

- (instancetype)initWithEmail:(NSString *)email point:(CGPoint)point;

@end

@interface STNDiffInsert : STNDiff

@end

@interface STNDiffRemove : STNDiff

@end

@interface STNDiffUpdate : STNDiff

@end
