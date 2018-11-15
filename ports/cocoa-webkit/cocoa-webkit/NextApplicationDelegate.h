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
- (bool)windowDelete:(NSString *)key;
- (NSString*)windowActive;
- (bool)setActiveBufferForWindow:(NSString *)window withBuffer:(NSString *)buffer;
- (NSString *)bufferMake;
- (bool)bufferDelete:(NSString *)key;
- (NSString *)bufferExecuteJavaScript:(NSString *)bufferKey withJavaScript:(NSString *) javaScript;
- (int)minibufferSetHeight:(int)height forWindow:(NSString *)key;
- (NSString *)minibufferExecuteJavaScript:(NSString *)windowKey withJavaScript:(NSString *)javaScript;


@end

