//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Server.h"
#import "NextApplicationDelegate.h"
#include "Global.h"

@implementation Server

- (void) start {
    [[[Global sharedInstance] port] intValue];
    NSSocketPort *server = [[NSSocketPort alloc] initWithTCPPort:1234];
    [server setDelegate: self];
    [[NSRunLoop mainRunLoop] addPort: server forMode: NSDefaultRunLoopMode];
    [[NSRunLoop mainRunLoop] run];
}

- (void)handlePortMessage:(NSPortMessage *)portMessage {
    printf("Message Received\n");
}

@end
