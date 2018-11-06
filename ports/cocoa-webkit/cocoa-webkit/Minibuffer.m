//
//  Minibuffer.m
//  next-cocoa
//
//  Created by John Mercouris on 3/5/18.
//  Copyright © 2018 Next. All rights reserved.
//

#import "Minibuffer.h"

@implementation Minibuffer

- (void)stringByEvaluatingJavaScriptFromString:(NSString *)script
{
    [self evaluateJavaScript:script completionHandler:^(id result, NSError *error) {
        if (error == nil) {
            if (result != nil) {
                NSLog(@"%@", [NSString stringWithFormat:@"%@", result]);
            }
        } else {
            NSLog(@"evaluateJavaScript error : %@", error.localizedDescription);
        }
    }];
}

@end
