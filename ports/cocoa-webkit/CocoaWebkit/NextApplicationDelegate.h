//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <AppKit/AppKit.h>
#import <Foundation/Foundation.h>

@interface NextApplicationDelegate : NSObject <NSApplicationDelegate>

@property (strong, atomic, readwrite) NSMutableDictionary* windows;
@property (strong, atomic, readwrite) NSMutableDictionary* buffers;

- (void)windowMake:(NSString*) key;
- (void)window:(NSString*)key setTitle:(NSString*)title;
- (bool)windowDelete:(NSString*)key;
- (NSString*)windowActive;
- (bool)windowExists:(NSString*)key;
- (bool)setActiveBufferForWindow:(NSString*)window buffer:(NSString*)buffer;
- (int)setMinibufferHeightForWindow:(NSString*)windowKey height:(NSNumber*)height;
- (void)bufferMake:(NSString*) key;
- (bool)bufferDelete:(NSString*)key;
- (NSString*)bufferEvaluateJavaScript:(NSString*)bufferKey javaScript:(NSString*)javaScript;
- (NSString*)minibufferEvaluateJavaScript:(NSString*)windowKey javaScript:(NSString*)javaScript;

@end
