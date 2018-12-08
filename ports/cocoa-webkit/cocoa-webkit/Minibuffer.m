//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Minibuffer.h"
#include "Global.h"
#include "XMLRPCRequest.h"


@implementation Minibuffer
@synthesize callBackCount;
@synthesize parentWindowIdentifier;

- (NSString *)evaluateJavaScript:(NSString *) javaScript
{
    [self setCallBackCount:[self callBackCount] + 1];
    [self evaluateJavaScript:javaScript completionHandler:^(id result, NSError *error) {
        if (error == nil && result != nil) {
            NSString* transformedResult = [NSString stringWithFormat:@"%@", result];
            NSString *coreSocket = [[Global sharedInstance] coreSocket];
            XMLRPCRequest *request = [[XMLRPCRequest alloc] initWithURL:
                                      [NSURL URLWithString:coreSocket]];
            [request setMethod:@"MINIBUFFER-JAVASCRIPT-CALL-BACK"
                withParameters:@[[self parentWindowIdentifier],
                                 transformedResult,
                                 [@([self callBackCount]) stringValue]]];
            NSURLSession *session = [NSURLSession sharedSession];
            [[session dataTaskWithRequest:[request request]] resume];
        } else if ([error description]) {
            NSLog(@"evaluateJavaScript error : %@", [error description]);
        }
    }];
    return [@([self callBackCount]) stringValue];
}


@end
