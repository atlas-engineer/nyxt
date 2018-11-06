//
//  Base.h
//  cocoa-webkit
//
//  Created by John Mercouris on 3/5/18.
//  Copyright © 2018 Atlas Engineer LLC. All rights reserved.
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
