//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Cocoa/Cocoa.h>
#import "Minibuffer.h"
#import "Buffer.h"


@interface Base : NSStackView

@property (assign) Buffer *buffer;
@property (assign) Minibuffer *minibuffer;
@property (assign) NSLayoutConstraint *minibufferHeightConstraint;

- (instancetype) init;
- (int)setMinibufferHeight:(int)height;
- (void)minibufferExecuteJavascript:(NSString *)javascript;
- (void)setActiveBuffer:(Buffer*)buffer;

@end
