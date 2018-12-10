//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import "Window.h"
#import "Base.h"
#include "Global.h"
#include "XMLRPCRequest.h"

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
    NSString *coreSocket = [[Global sharedInstance] coreSocket];
    XMLRPCRequest *request = [[XMLRPCRequest alloc] initWithURL:
                              [NSURL URLWithString:coreSocket]];
    [request setMethod:@"WINDOW-WILL-CLOSE"
        withParameters:@[[self identifier]]];
    NSURLSession *session = [NSURLSession sharedSession];
    [[session dataTaskWithRequest:[request request]] resume];
}

@end
