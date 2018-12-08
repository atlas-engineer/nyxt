//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>
#import "GCDAsyncSocket.h"

@interface Server : NSObject <GCDAsyncSocketDelegate>
{
    GCDAsyncSocket *socket;
}

- (void) start;

@end
