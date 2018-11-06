//
//  Minibuffer.h
//  cocoa-webkit
//
//  Created by John Mercouris on 3/5/18.
//  Copyright © 2018 Atlas Engineer LLC. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

@interface Minibuffer : WKWebView

- (void)stringByEvaluatingJavaScriptFromString:(NSString *)script;

@end
