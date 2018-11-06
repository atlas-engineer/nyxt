//
//  Minibuffer.h
//  next-cocoa
//
//  Created by John Mercouris on 3/5/18.
//  Copyright © 2018 Next. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

@interface Minibuffer : WKWebView

- (void)stringByEvaluatingJavaScriptFromString:(NSString *)script;

@end
