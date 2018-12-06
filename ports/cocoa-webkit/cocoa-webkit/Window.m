//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import "Window.h"
#import "Base.h"
#include <xmlrpc-c/base.h>
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/config.h>
#include "Global.h"

@implementation Window
@synthesize base;
@synthesize identifier;

- (id) init {
    self = [super init];
    if (self != nil) {
        NSRect windowRect = NSMakeRect(0, 0, 1024, 768);
        NSUInteger windowStyle = NSWindowStyleMaskTitled | NSWindowStyleMaskResizable |
        NSWindowStyleMaskMiniaturizable | NSWindowStyleMaskClosable;
        
        [self setDelegate:self];
        [self setFrame:windowRect display:YES];
        [self setBackingType:NSBackingStoreBuffered];
        [self setStyleMask:windowStyle];
        [self setOpaque:YES];
        [self setHasShadow:YES];
        [self setTitle:@"Next"];
        [self setBackgroundColor:[NSColor whiteColor]];
        [self makeKeyAndOrderFront:self];
        [self setCollectionBehavior:NSWindowCollectionBehaviorFullScreenPrimary];
        
        Base *baseView = [[Base alloc] init];
        [baseView setFrame:[[self contentView] bounds]];
        [self setContentView:baseView];
        [self setReleasedWhenClosed:NO];
        
        [self setBase: baseView];
    }
    return self;
}

- (void)windowWillClose:(NSNotification *)notification {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0), ^{
        xmlrpc_env env = [[Global sharedInstance] getXMLRPCEnv];
        const char * const serverUrl = [[[Global sharedInstance] coreSocket] UTF8String];
        const char * const methodName = "WINDOW-WILL-CLOSE";
        
        // Make the remote procedure call
        xmlrpc_client_call(&env, serverUrl, methodName,
                           "(s)",
                           [[self identifier] UTF8String]);
    });
}

@end
