//
//  Buffer.h
//  cocoa-webkit
//
//  Created by John Mercouris on 2/28/18.
//  Copyright © 2018 Atlas Engineer LLC. All rights reserved.
//

#import <WebKit/WebKit.h>

@interface Buffer : NSStackView

@property (assign) WKWebView *webView;

- (instancetype) init;
- (void)setURL:(NSString *)URL;


@end
