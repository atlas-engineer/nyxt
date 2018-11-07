//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
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
    [self setURL:@"file:///Users/jmercouris/Downloads/webpage.html"];
    
    return self;
}

- (void)setURL:(NSString *)URL
{
    NSURLRequest *urlRequest = [NSURLRequest requestWithURL:[NSURL URLWithString:URL]];
    [self.webView loadRequest:urlRequest];
}

@end
