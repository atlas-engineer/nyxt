//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import <Cocoa/Cocoa.h>
#import "Base.h"


@interface Window : NSObject

@property (strong, atomic, readwrite) NSWindow *window;
@property (strong, atomic, readwrite) Base *base;

- (id) init;
- (void) close;
- (void) setActiveBuffer:(Buffer*) buffer;

@end

