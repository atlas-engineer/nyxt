//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <WebKit/WebKit.h>

@interface Buffer : WKWebView

@property (nonatomic, readwrite) NSInteger callBackCount;
@property (strong, atomic, readwrite) NSString *identifier;

- (instancetype) init;
- (NSString *)evaluateJavaScript:(NSString *) javaScript;
@end
