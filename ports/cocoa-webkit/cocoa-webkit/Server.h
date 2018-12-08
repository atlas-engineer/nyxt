//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>
#import "GCDAsyncSocket.h"

@interface Server : NSObject <GCDAsyncSocketDelegate>
{
    dispatch_queue_t socketQueue;
    
    GCDAsyncSocket *listenSocket;
    NSMutableArray *connectedSockets;
    
    BOOL isRunning;
}

- (void) start;

@end
