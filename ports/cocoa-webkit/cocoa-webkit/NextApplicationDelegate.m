//
//  NextApplicationDelegate.m
//  next-cocoa
//
//  Created by John Mercouris on 3/13/18.
//  Copyright © 2018 Next. All rights reserved.
//

#import "NextApplicationDelegate.h"
#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>
#import <Webkit/Webkit.h>

#import "Server.h"
#import "Base.h"
#import "Buffer.h"

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
    NSRect windowRect = NSMakeRect(0, 0, 1024, 768);
    NSUInteger windowStyle = NSWindowStyleMaskTitled | NSWindowStyleMaskResizable;
    NSWindow *window = [[NSWindow alloc]
                        initWithContentRect:windowRect
                        styleMask:windowStyle
                        backing:NSBackingStoreBuffered
                        defer:NO];
    [window setOpaque:YES];
    [window setHasShadow:YES];
    [window setTitle:@"Next"];
    [window setBackgroundColor:[NSColor whiteColor]];
    [window makeKeyAndOrderFront:self];

    Base *baseView = [[Base alloc] init];
    [baseView setFrame:[[window contentView] bounds]];
    [window setContentView:baseView];

    return [[self windows] insertElement:window];
}

- (bool)windowClose:(NSString *)key
{
    NSWindow *window = [[self windows] objectForKey:key];
    [window setReleasedWhenClosed:NO];
    [window close];
    [[self windows] removeObjectForKey:key];
    return YES;
}

- (NSString*)windowActive
{
    NSWindow *window;
    NSWindow *activeWindow;
    activeWindow = [[NSApplication sharedApplication] keyWindow];
    for (id key in [[self windows] allKeys]) {
        window = [[self windows] objectForKey:key];
        if (activeWindow == window) {
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

- (int)minibufferSetHeight:(int)height forWindow:(NSString *)key
{
    return [[[[self windows] objectForKey:key] contentView] setMinibufferHeight: height];
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

