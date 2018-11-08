//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "NextApplicationDelegate.h"
#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>
#import <Webkit/Webkit.h>

#import "Server.h"
#import "Base.h"
#import "Buffer.h"
#import "Window.h"

@implementation NextApplicationDelegate
@synthesize windows;
@synthesize buffers;

- (id) init
{
    self = [super init];
    if (self) {
        [self setWindows:[[AutokeyDictionary alloc] init]];
        [self setBuffers:[[AutokeyDictionary alloc] init]];
    }
    return self;
}

- (NSString *)windowMake
{
    Window *window = [[Window alloc] init];
    return [[self windows] insertElement:window];
}

- (bool)windowClose:(NSString *)key
{
    Window *window = [[self windows] objectForKey:key];
    [window close];
    [[self windows] removeObjectForKey:key];
    return YES;
}

- (NSString*)windowActive
{
    NSWindow *nswindow;
    NSWindow *activeWindow;
    Window *window;
    activeWindow = [[NSApplication sharedApplication] keyWindow];

    for (id key in [[self windows] allKeys]) {
        window = [[self windows] objectForKey:key];
        nswindow = [window window];
        window = [[self windows] objectForKey:key];
        if (activeWindow == nswindow) {
            return key;
        }
    }

    return @"-1"; // Return Error code
}

- (NSString *)bufferMake
{
    Buffer *buffer = [[Buffer alloc] init];
    return [[self buffers] insertElement:buffer];
}

- (bool)setActiveBufferForWindow:(NSString *)windowKey withBuffer:(NSString *)bufferKey
{
    Window *window = [[self windows] objectForKey:windowKey];
    Buffer *buffer = [[self buffers] objectForKey:bufferKey];

    [[window base] setActiveBuffer:buffer];
    return true;
}

- (int)minibufferSetHeight:(int)height forWindow:(NSString *)windowKey
{
    Window *window = [[self windows] objectForKey:windowKey];
    
    return [[window base] setMinibufferHeight:height];
}

- (void)minibufferExecuteJavascript:(NSString *)javascript
                                forWindow:(NSString *)key
{
    [[[[self windows] objectForKey:key] contentView]
     minibufferExecuteJavascript: javascript];
}

- (void)applicationDidFinishLaunching:(NSNotification*)aNotification
{
    Server *server = [[Server alloc] init];
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{
        [server start];
    });
}

@end

