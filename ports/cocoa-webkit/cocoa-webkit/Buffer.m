//
//  Buffer.m
//  cocoa-webkit
//
//  Created by John Mercouris on 2/28/18.
//  Copyright © 2018 Atlas Engineer LLC. All rights reserved.
//

#import "Buffer.h"

@implementation Buffer
@synthesize webView;

- (void)drawRect:(NSRect)dirtyRect {
    [[NSColor blackColor] setFill];
    NSRectFill(dirtyRect);
    [super drawRect:dirtyRect];
}

- (instancetype)init
{
    self = [super init];
    [self setTranslatesAutoresizingMaskIntoConstraints:NO];
    [self setOrientation:NSUserInterfaceLayoutOrientationVertical];
    [self setSpacing:0.0];

    [self setWebView:[[WKWebView alloc] init]];
    [self.webView setTranslatesAutoresizingMaskIntoConstraints:NO];
        
    [self addArrangedSubview:self.webView];
    
    return self;
}

- (void)setURL:(NSString *)URL
{
    NSURLRequest *urlRequest = [NSURLRequest requestWithURL:[NSURL URLWithString:URL]];
    [self.webView loadRequest:urlRequest];
}

@end
