//
//  FCTWebSocket.h
//  Trellis
//
//  Created by Babak Ghahremanpour on 11/28/12.
//
//

#import <Foundation/Foundation.h>

@interface FCTWebSocket : NSObject

@property (nonatomic, readonly) RACSignal *messageSignal;
@property (nonatomic, readonly) RACSignal *openedSignal;

- (instancetype)initWithJSONSignal:(RACSignal *)JSONSignal;

- (void)start;

@end