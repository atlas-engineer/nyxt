//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>
#import "AutokeyDictionary.h"


@interface NextApplicationDelegate : NSObject <NSApplicationDelegate>

@property (strong, atomic, readwrite) AutokeyDictionary *windows;
@property (strong, atomic, readwrite) AutokeyDictionary *buffers;

- (NSString *)windowMake;
- (bool)windowClose:(NSString *)key;
- (NSString*)windowActive;
- (bool)setActiveBufferForWindow:(NSString *)window withBuffer:(NSString *)buffer;
- (NSString *)bufferMake;
- (NSString *)bufferExecuteJavascript:(NSString *)bufferKey withJavascript:(NSString *) javaScript;
- (int)minibufferSetHeight:(int)height forWindow:(NSString *)key;
- (void)minibufferExecuteJavascript:(NSString *)javascript forWindow:(NSString *)key;


@end

