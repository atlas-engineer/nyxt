//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Buffer.h"
#import "Minibuffer.h"
#import <Cocoa/Cocoa.h>

@interface Base : NSStackView

@property (strong, atomic, readwrite) Buffer* buffer;
@property (strong, atomic, readwrite) Minibuffer* minibuffer;
@property (strong, atomic, readwrite) NSLayoutConstraint* minibufferHeightConstraint;

- (instancetype)init;
- (int)setMinibufferHeight:(int)height;
- (void)setActiveBuffer:(Buffer*)buffer;

@end
