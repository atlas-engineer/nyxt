//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Cocoa/Cocoa.h>
#import "Minibuffer.h"

@interface Base : NSStackView

@property (assign) Minibuffer *minibuffer;
@property (assign) NSLayoutConstraint *minibufferHeightConstraint;

- (instancetype) init;
- (int)setMinibufferHeight:(int)height;
- (void)minibufferExecuteJavascript:(NSString *)javascript;

@end
