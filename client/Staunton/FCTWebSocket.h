//
//  FCTWebSocket.h
//  Trellis
//
//  Created by Babak Ghahremanpour on 11/28/12.
//
//

#import <Foundation/Foundation.h>

@interface FCTWebSocket : NSObject

@property (nonatomic, readonly) RACSignal *notificationSignal; // of FCTWebSocketNotification

//! Called whenever the socket successfully opens.
@property (nonatomic, copy) void (^openedBlock)(void);
@property (nonatomic, copy) void (^messageBlock)(NSDictionary *message);

- (void)start;

@end