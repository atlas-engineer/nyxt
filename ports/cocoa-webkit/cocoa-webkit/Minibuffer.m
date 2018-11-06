//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
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
