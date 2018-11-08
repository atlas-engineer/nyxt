//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Cocoa/Cocoa.h>
#import "Minibuffer.h"
#import "Buffer.h"


@interface Base : NSStackView

@property (strong, atomic, readwrite) Buffer *buffer;
@property (strong, atomic, readwrite) Minibuffer *minibuffer;
@property (strong, atomic, readwrite) NSLayoutConstraint *minibufferHeightConstraint;

- (instancetype) init;
- (int)setMinibufferHeight:(int)height;
- (void)minibufferExecuteJavascript:(NSString *)javascript;
- (void)setActiveBuffer:(Buffer*)buffer;

@end
