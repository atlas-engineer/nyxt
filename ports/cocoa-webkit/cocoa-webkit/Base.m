//
//  Base.m
//  next-cocoa
//
//  Created by John Mercouris on 3/5/18.
//  Copyright © 2018 Next. All rights reserved.
//

#import "Base.h"
#import "Buffer.h"
#import "Minibuffer.h"

@implementation Base

@synthesize minibuffer;
@synthesize minibufferHeightConstraint;

- (instancetype)init
{
    self = [super init];
    [self setOrientation:NSUserInterfaceLayoutOrientationVertical];
    [self setTranslatesAutoresizingMaskIntoConstraints:NO];
    [self setMinibuffer:[[Minibuffer alloc] init]];
    [self setMinibufferHeightConstraint:
     [NSLayoutConstraint constraintWithItem:self.minibuffer
                                  attribute:NSLayoutAttributeHeight
                                  relatedBy:NSLayoutRelationEqual
                                     toItem:nil
                                  attribute:NSLayoutAttributeNotAnAttribute
                                 multiplier:1.0f
                                   constant:10]];

    Buffer *buffer = [[Buffer alloc] init];
    [buffer setURL:@"file:///Users/jmercouris/Downloads/webpage.html"];

    [[self minibuffer] addConstraint:[self minibufferHeightConstraint]];

    [self addArrangedSubview:buffer];
    [self addArrangedSubview:minibuffer];
    
    return self;
}

- (int)setMinibufferHeight:(int)height
{
    [[self minibufferHeightConstraint] setConstant:height];
    return height;
}

- (void)minibufferExecuteJavascript:(NSString *)javascript
{
    [[self minibuffer] stringByEvaluatingJavaScriptFromString:javascript];
}

@end
