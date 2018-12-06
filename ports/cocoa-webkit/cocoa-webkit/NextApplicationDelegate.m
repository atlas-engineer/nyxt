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
#import "Global.h"

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
    [window setIdentifier: [[self windows] insertElement:window]];
    [[[window base] minibuffer] setParentWindowIdentifier: [window identifier]];
    return [window identifier];
}

- (void)window:(NSString *)key setTitle:(NSString *)title
{
    Window *window = [[self windows] objectForKey:key];
    [window setTitle:title];
}

- (bool)windowDelete:(NSString *)key
{
    Window *window = [[self windows] objectForKey:key];
    [window close];
    [[self windows] removeObjectForKey:key];
    return YES;
}

- (NSString*)windowActive
{
    NSWindow *activeWindow = [[NSApplication sharedApplication] keyWindow];
    if (activeWindow) {
        return [activeWindow identifier];
    }
    return @"<no active window>";
}

- (bool)windowExists:(NSString *)key
{
    Window *window = [[self windows] objectForKey:key];
    if (window) {
        return YES;
    } else {
        return NO;
    }
}

- (int)setMinibufferHeightForWindow:(NSString *)windowKey height:(int)height
{
    Window *window = [[self windows] objectForKey:windowKey];
    return [[window base] setMinibufferHeight:height];
}

- (bool)setActiveBufferForWindow:(NSString *)windowKey buffer:(NSString *)bufferKey
{
    Window *window = [[self windows] objectForKey:windowKey];
    Buffer *buffer = [[self buffers] objectForKey:bufferKey];
    [[window base] setActiveBuffer:buffer];
    return true;
}

- (NSString *)bufferMake
{
    Buffer *buffer = [[Buffer alloc] init];
    [buffer setIdentifier: [[self buffers] insertElement:buffer]];
    return [buffer identifier];
}

- (bool)bufferDelete:(NSString *)key
{
    [[self buffers] removeObjectForKey:key];
    return YES;
}

- (NSString *)bufferEvaluateJavaScript:(NSString *)bufferKey javaScript:(NSString *) javaScript
{
    Buffer *buffer = [[self buffers] objectForKey:bufferKey];
    return [buffer evaluateJavaScript:javaScript];
}

- (NSString *)minibufferEvaluateJavaScript:(NSString *)windowKey javaScript:(NSString *)javaScript
{
    Window *window = [[self windows] objectForKey:windowKey];
    Minibuffer *minibuffer = [[window base] minibuffer];
    return [minibuffer evaluateJavaScript:javaScript];
}

- (void)applicationDidFinishLaunching:(NSNotification*)aNotification
{
    Server *server = [[Server alloc] init];
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        [server start];
    });
}

@end
