//
//  NextApplicationDelegate.h
//  next-cocoa
//
//  Created by John Mercouris on 3/13/18.
//  Copyright © 2018 Next. All rights reserved.
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
- (NSString *)bufferMake;
- (int)minibufferSetHeight:(int)height forWindow:(NSString *)key;
- (void)minibufferExecuteJavascript:(NSString *)javascript
                          forWindow:(NSString *)key;

@end

