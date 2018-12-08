//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Buffer.h"
#include "Global.h"
#include "XMLRPCRequest.h"

@implementation Buffer
@synthesize callBackCount;
@synthesize identifier;

- (instancetype)init
{
    self = [super init];
    [self setTranslatesAutoresizingMaskIntoConstraints:NO];
    [self setCallBackCount:0];
    [self setNavigationDelegate:self];
    return self;
}

- (NSString *)evaluateJavaScript:(NSString *) javaScript
{
    [self setCallBackCount:[self callBackCount] + 1];
    [self evaluateJavaScript:javaScript completionHandler:^(id result, NSError *error) {
        if (error == nil && result != nil) {
            NSString* transformedResult = [NSString stringWithFormat:@"%@", result];
            NSString *coreSocket = [[Global sharedInstance] coreSocket];
            XMLRPCRequest *request = [[XMLRPCRequest alloc] initWithURL:
                                      [NSURL URLWithString:coreSocket]];
            [request setMethod:@"BUFFER-JAVASCRIPT-CALL-BACK"
                withParameters:@[[self identifier],
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

- (void)webView:(WKWebView *)webView didCommitNavigation:(WKNavigation *)navigation {
    NSString *url = [[self URL] absoluteString];
    NSString *coreSocket = [[Global sharedInstance] coreSocket];
    XMLRPCRequest *request = [[XMLRPCRequest alloc] initWithURL:
                              [NSURL URLWithString:coreSocket]];
    [request setMethod:@"BUFFER-DID-COMMIT-NAVIGATION"
        withParameters:@[[self identifier], url]];
    NSURLSession *session = [NSURLSession sharedSession];
    [[session dataTaskWithRequest:[request request]] resume];
}

@end
