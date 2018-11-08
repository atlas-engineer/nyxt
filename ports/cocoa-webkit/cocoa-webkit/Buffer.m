//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Buffer.h"

@implementation Buffer

- (instancetype)init
{
    self = [super init];
    [self setTranslatesAutoresizingMaskIntoConstraints:NO];
    [self setURL:@"file:///Users/jmercouris/Downloads/webpage.html"];
    return self;
}

- (void)stringByEvaluatingJavaScriptFromString:(NSString *)script
{
    [self evaluateJavaScript:script completionHandler:^(id result, NSError *error) {
        if (error == nil) {
            if (result != nil) {
                // XML RPC CLIENT CALL HERE!!!
                NSLog(@"%@", [NSString stringWithFormat:@"%@", result]);
            }
        } else {
            NSLog(@"evaluateJavaScript error : %@", error.localizedDescription);
        }
    }];
}

- (void)setURL:(NSString *)URL
{
    NSURLRequest *urlRequest = [NSURLRequest requestWithURL:[NSURL URLWithString:URL]];
    [self loadRequest:urlRequest];
}

@end
