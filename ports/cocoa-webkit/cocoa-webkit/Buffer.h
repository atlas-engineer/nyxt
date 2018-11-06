//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <WebKit/WebKit.h>

@interface Buffer : NSStackView

@property (assign) WKWebView *webView;

- (instancetype) init;
- (void)setURL:(NSString *)URL;


@end
