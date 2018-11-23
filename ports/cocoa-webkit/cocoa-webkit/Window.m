//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import "Window.h"
#import "Base.h"

@implementation Window
@synthesize base;
@synthesize identifier;

- (id) init {
    self = [super init];
    if (self != nil) {
        NSRect windowRect = NSMakeRect(0, 0, 1024, 768);
        NSUInteger windowStyle = NSWindowStyleMaskTitled | NSWindowStyleMaskResizable | NSWindowStyleMaskMiniaturizable;

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

@end
