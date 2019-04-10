//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "XMLRPCDecoder.h"
#import "XMLRPCElementDecoder.h"
#import <Foundation/Foundation.h>

@interface XMLRPCDefaultRequestDecoder : NSObject <XMLRPCDecoder, NSXMLParserDelegate> {
    NSXMLParser* parser;
    NSString* method;
    NSMutableArray* stack;
    NSMutableArray* insertionStack;

    XMLRPCElementType elementType;
    NSString* elementKey;
    id elementValue;
}

- (NSString*)method;
- (NSArray*)parameters;

@end
