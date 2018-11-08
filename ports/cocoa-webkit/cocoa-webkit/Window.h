//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import <Cocoa/Cocoa.h>
#import "Base.h"


@interface Window : NSWindow

@property (strong, atomic, readwrite) Base *base;
@property (strong, atomic, readwrite) NSString *identifier;

- (id) init;

@end

