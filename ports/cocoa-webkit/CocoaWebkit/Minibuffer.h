//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

@interface Minibuffer : WKWebView

@property (nonatomic, readwrite) NSInteger callBackCount;
@property (strong, atomic, readwrite) NSString* parentWindowIdentifier;

- (NSString*)evaluateJavaScript:(NSString*)javaScript;

@end
