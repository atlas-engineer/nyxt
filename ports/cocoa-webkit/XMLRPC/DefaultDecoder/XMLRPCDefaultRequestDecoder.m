//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import "XMLRPCDefaultRequestDecoder.h"

@implementation XMLRPCDefaultRequestDecoder

- (id)init {
    if (self = [super init]) {
        method = [[NSString alloc] init];
        parameters = [[NSMutableArray alloc] init];
        
        elementType = XMLRPCElementTypeString;
        elementKey = nil;
        elementValue = [[NSMutableString alloc] init];
    }
    return self;
}

- (void)decodeWithData:(NSData*) data {
    parser = [[NSXMLParser alloc] initWithData:data];
    [parser setDelegate:self];
    [parser setShouldResolveExternalEntities: YES];
    [parser parse];
}

- (NSString *)method {
    return method;
}

- (NSArray *)parameters {
    return parameters;
}

- (void)setElementType: (XMLRPCElementType)element {
    elementType = element;
}

- (XMLRPCElementType)elementType {
    return elementType;
}

- (void)parser: (NSXMLParser *)parser foundCharacters: (NSString *)string {
    if (!elementValue) {
        elementValue = [[NSMutableString alloc] initWithString: string];
    } else {
        [elementValue appendString: string];
    }
}

- (void)parser:(NSXMLParser*)parser
didStartElement:(NSString*)element
  namespaceURI:(NSString*)namespaceURI
 qualifiedName:(NSString*)qualifiedName
    attributes:(NSDictionary*)attributeDict {
    if ([element isEqualToString: @"array"]) {
        [self setElementType: XMLRPCElementTypeArray];
    } else if ([element isEqualToString: @"methodName"]) {
        [self setElementType: XMLRPCElementTypeMethodName];
    } else if ([element isEqualToString: @"struct"]) {
        [self setElementType: XMLRPCElementTypeDictionary];
    } else if ([element isEqualToString: @"int"] || [element isEqualToString: @"i4"]) {
        [self setElementType: XMLRPCElementTypeInteger];
    } else if ([element isEqualToString: @"double"]) {
        [self setElementType: XMLRPCElementTypeDouble];
    } else if ([element isEqualToString: @"boolean"]) {
        [self setElementType: XMLRPCElementTypeBoolean];
    } else if ([element isEqualToString: @"string"]) {
        [self setElementType: XMLRPCElementTypeString];
    } else if ([element isEqualToString: @"dateTime.iso8601"]) {
        [self setElementType: XMLRPCElementTypeDate];
    } else if ([element isEqualToString: @"base64"]) {
        [self setElementType: XMLRPCElementTypeData];
    }
}

- (void)parser:(NSXMLParser *)parser
 didEndElement:(NSString *)element
  namespaceURI: (NSString *)namespaceURI
 qualifiedName: (NSString *)qualifiedName {
    NSString *tmpElement;
    tmpElement = [XMLRPCElementParser parseString: elementValue];
    elementValue = nil;

    switch (elementType) {
        case XMLRPCElementTypeMethodName:
            method = tmpElement;
            break;
        default:
            break;
    }
}

@end
