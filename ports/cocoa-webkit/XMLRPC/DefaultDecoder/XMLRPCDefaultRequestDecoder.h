//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import <Foundation/Foundation.h>
#import "XMLRPCDecoder.h"
#import "XMLRPCElementParser.h"

@interface XMLRPCDefaultRequestDecoder : NSObject <XMLRPCDecoder, NSXMLParserDelegate> {
    NSXMLParser *parser;
    NSString *method;
    NSMutableArray *parameters;
    
    XMLRPCElementType elementType;
    NSString *elementKey;
    id elementValue;
}

- (NSString *)method;
- (NSArray *)parameters;

@end
