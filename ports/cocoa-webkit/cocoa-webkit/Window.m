//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import "Window.h"
#import "Base.h"

@implementation Window
@synthesize window;
@synthesize base;

- (id) init {
    self = [super init];
    if (self != nil) {
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
        
        [self setBase: baseView];
        [self setWindow: window];
    }
    return self;
}

- (void) close {
    [[self window] setReleasedWhenClosed:NO];
    [[self window] close];
}

@end
