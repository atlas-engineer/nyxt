#import "XMLRPCEventBasedParserDelegate.h"
#import "NSData+Base64.h"

@interface XMLRPCEventBasedParserDelegate (XMLRPCEventBasedParserDelegatePrivate)

- (BOOL)isDictionaryElementType: (XMLRPCElementType)elementType;

- (void)addElementValueToParent;

- (NSDate *)parseDateString: (NSString *)dateString withFormat: (NSString *)format;

- (NSNumber *)parseInteger: (NSString *)value;

- (NSNumber *)parseDouble: (NSString *)value;

- (NSNumber *)parseBoolean: (NSString *)value;

- (NSString *)parseString: (NSString *)value;

- (NSDate *)parseDate: (NSString *)value;

- (NSData *)parseData: (NSString *)value;

@end

@implementation XMLRPCEventBasedParserDelegate

- (id)initWithParent: (XMLRPCEventBasedParserDelegate *)parent {
    self = [super init];
    if (self) {
        myParent = parent;
        myChildren = [[NSMutableSet alloc] initWithCapacity: 1];
        myElementType = XMLRPCElementTypeString;
        myElementKey = nil;
        myElementValue = [[NSMutableString alloc] init];
    }
    
    return self;
}

- (void)setParent: (XMLRPCEventBasedParserDelegate *)parent {
    myParent = parent;
}

- (XMLRPCEventBasedParserDelegate *)parent {
    return myParent;
}

- (void)setElementType: (XMLRPCElementType)elementType {
    myElementType = elementType;
}

- (XMLRPCElementType)elementType {
    return myElementType;
}

- (void)setElementKey: (NSString *)elementKey {
    myElementKey = elementKey;
}

- (NSString *)elementKey {
    return myElementKey;
}

- (void)setElementValue: (id)elementValue {
    myElementValue = elementValue;
}

- (id)elementValue {
    return myElementValue;
}

@end

@implementation XMLRPCEventBasedParserDelegate (NSXMLParserDelegate)

- (void)parser: (NSXMLParser *)parser didStartElement: (NSString *)element namespaceURI: (NSString *)namespaceURI qualifiedName: (NSString *)qualifiedName attributes: (NSDictionary *)attributes {
    if ([element isEqualToString: @"value"] || [element isEqualToString: @"member"] || [element isEqualToString: @"name"]) {
        XMLRPCEventBasedParserDelegate *parserDelegate = [[XMLRPCEventBasedParserDelegate alloc] initWithParent: self];
        if ([element isEqualToString: @"member"]) {
            [parserDelegate setElementType: XMLRPCElementTypeMember];
        } else if ([element isEqualToString: @"name"]) {
            [parserDelegate setElementType: XMLRPCElementTypeName];
        }
        [myChildren addObject: parserDelegate];
        [parser setDelegate: parserDelegate];
        return;
    }
    
    if ([element isEqualToString: @"array"]) {
        NSMutableArray *array = [[NSMutableArray alloc] init];
        
        [self setElementValue: array];
        [self setElementType: XMLRPCElementTypeArray];
    } else if ([element isEqualToString: @"struct"]) {
        NSMutableDictionary *dictionary = [[NSMutableDictionary alloc] init];
        
        [self setElementValue: dictionary];
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

- (void)parser: (NSXMLParser *)parser didEndElement: (NSString *)element namespaceURI: (NSString *)namespaceURI qualifiedName: (NSString *)qualifiedName {
    if ([element isEqualToString: @"value"] || [element isEqualToString: @"member"] || [element isEqualToString: @"name"]) {
        NSString *elementValue = nil;
        
        if ((myElementType != XMLRPCElementTypeArray) && ![self isDictionaryElementType: myElementType]) {
            elementValue = [self parseString: myElementValue];
            myElementValue = nil;
        }
        
        switch (myElementType) {
            case XMLRPCElementTypeInteger:
                myElementValue = [self parseInteger: elementValue];
                break;
            case XMLRPCElementTypeDouble:
                myElementValue = [self parseDouble: elementValue];
                break;
            case XMLRPCElementTypeBoolean:
                myElementValue = [self parseBoolean: elementValue];
                break;
            case XMLRPCElementTypeString:
            case XMLRPCElementTypeName:
                myElementValue = elementValue;
                break;
            case XMLRPCElementTypeDate:
                myElementValue = [self parseDate: elementValue];
                break;
            case XMLRPCElementTypeData:
                myElementValue = [self parseData: elementValue];
                break;
            default:
                break;
        }
        
        if (myParent && myElementValue) {
            [self addElementValueToParent];
        }
        
        [parser setDelegate: myParent];

        if (myParent) {
            XMLRPCEventBasedParserDelegate *parent = myParent;

            // Set it to nil explicitly since it's not __weak but __unsafe_unretained.
            // We're doing it here because if we'll do it after removal from myChildren
            // self can already be deallocated, and accessing field of deallocated object
            // causes memory corruption.
            myParent = nil;

            [parent->myChildren removeObject: self];
        }
    }
}

- (void)parser: (NSXMLParser *)parser foundCharacters: (NSString *)string {
    if ((myElementType == XMLRPCElementTypeArray) || [self isDictionaryElementType: myElementType]) {
        return;
    }
    
    if (!myElementValue) {
        myElementValue = [[NSMutableString alloc] initWithString: string];
    } else {
        [myElementValue appendString: string];
    }
}

- (void)parser: (NSXMLParser *)parser parseErrorOccurred: (NSError *)parseError {
    [parser abortParsing];
}

@end

@implementation XMLRPCEventBasedParserDelegate (XMLRPCEventBasedParserDelegatePrivate)

- (BOOL)isDictionaryElementType: (XMLRPCElementType)elementType {
    if ((myElementType == XMLRPCElementTypeDictionary) || (myElementType == XMLRPCElementTypeMember)) {
        return YES;
    }
    
    return NO;
}

- (void)addElementValueToParent {
    id parentElementValue = [myParent elementValue];
    
    switch ([myParent elementType]) {
        case XMLRPCElementTypeArray:
            [parentElementValue addObject: myElementValue];
            
            break;
        case XMLRPCElementTypeDictionary:
            if ([myElementValue isEqual:[NSNull null]]) {
                [parentElementValue removeObjectForKey:myElementKey];
            } else {
                [parentElementValue setObject: myElementValue forKey: myElementKey];
            }
            
            break;
        case XMLRPCElementTypeMember:
            if (myElementType == XMLRPCElementTypeName) {
                [myParent setElementKey: myElementValue];
            } else {
                [myParent setElementValue: myElementValue];
            }
            
            break;
        default:
            break;
    }
}

- (NSDate *)parseDateString: (NSString *)dateString withFormat: (NSString *)format {
    NSDateFormatter *dateFormatter = [[NSDateFormatter alloc] init];
    NSDate *result = nil;
    [dateFormatter setDateFormat: format];
    result = [dateFormatter dateFromString: dateString];
    return result;
}

- (NSNumber *)parseInteger: (NSString *)value {
    return [NSNumber numberWithInteger: [value integerValue]];
}

- (NSNumber *)parseDouble: (NSString *)value {
    return [NSNumber numberWithDouble: [value doubleValue]];
}

- (NSNumber *)parseBoolean: (NSString *)value {
    if ([value isEqualToString: @"1"]) {
        return [NSNumber numberWithBool: YES];
    }
    
    return [NSNumber numberWithBool: NO];
}

- (NSString *)parseString: (NSString *)value {
    return [value stringByTrimmingCharactersInSet: [NSCharacterSet whitespaceAndNewlineCharacterSet]];
}

- (NSDate *)parseDate: (NSString *)value {
    NSDate *result = nil;
    
    result = [self parseDateString: value withFormat: @"yyyyMMdd'T'HH:mm:ss"];
    
    if (!result) {
        result = [self parseDateString: value withFormat: @"yyyy'-'MM'-'dd'T'HH:mm:ss"];
    }
    
    if (!result) {
        result = [self parseDateString: value withFormat: @"yyyy'-'MM'-'dd'T'HH:mm:ssZ"];
    }
    
    if (!result) {
        result = (NSDate *)[NSNull null];
    }

    return result;
}

- (NSData *)parseData: (NSString *)value {
    return [NSData dataFromBase64String: value];
}

@end
